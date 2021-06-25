*          DATA SET DDNEWT     AT LEVEL 017 AS OF 04/08/18                      
*PHASE DDNEWT                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE DECODE                                                                 
*INCLUDE BUFFERIN                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'ACCPAK COMPARE RECORDS'                                         
         PRINT NOGEN                                                            
         USING DPRINT,R8                                                        
ACCOMP   CSECT                                                                  
         NBASE 0,*CMP*,=V(REGSAVE),R9                                           
         L     R8,CPRINT                                                        
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA                                                       
                                                                                
         GOTOR STXITER,DMCB,DMPL1                                               
         BRAS  RE,INIT                                                          
                                                                                
***********************************************************************         
* Input can be dump tape or disk. Either can be old or new                      
* Only one can be DISK, or both can be tape                                     
* If recovery then both are recovery tapes                                      
***********************************************************************         
* OLD=TAPE or DISK    (Either OLD or NEW can be DISK not both)                  
* NEW=TAPE or DISK                                                              
*                                                                               
* AGENCY= Alpha, will open DISK for this file                                   
*                                                                               
* Up to 6 pairs of filter keys can be used or one START= or END=                
* START= filter key paris with END=                                             
* END=   fitler key pairs sith START=                                           
*                                                                               
*****TYPE=comma delimiter numbers to filter record types                        
*                                                                               
* UPSI=Can be special debug switches for now                                    
*                                                                               
* PASSIVE=YES   Default no. Compare passive keys                                
*                                                                               
***********************************************************************         
         GOTOR OPENFIL,OLDFILE                                                  
         GOTOR OPENFIL,NEWFILE     OPEN INPUT FILE                              
         MVC   KEYDEL,=AL2(ACCRSTA-ACCRECD)                                     
         MVC   KEYCMPLN,=AL2(10)                                                
         MVC   KEYLEN,=AL2(10)                                                  
         TM    OLDFILE,RECOVERY    Recovery tapes?                              
         JZ    ACCMP100            No                                           
         TM    NEWFILE,RECOVERY                                                 
         JNZ   *+6                 Both must be on at this point                
         DC    H'00'                                                            
                                                                                
         GOTOR RCV2DMP,OLDFILE     Convert recovery to dump tape Old            
         GOTOR RCV2DMP,NEWFILE     Convert recovery to dump tape New            
         GOTOR BUFFRIN,DMCB,('BUFFACLO',KEYBUFF),AIO1,COMFACS                   
                                                                                
*------------------------------------------*                                    
* Re-open dump type tapes we just created  *                                    
*------------------------------------------*                                    
         GOTOR OPENFIL,OLDFILE                                                  
         GOTOR OPENFIL,NEWFILE                                                  
                                                                                
*----------------------------*                                                  
* Start with OLD file first  *                                                  
*----------------------------*                                                  
ACCMP100 GOTOR GETREC,OLDFILE      Get input record                             
         JNE   ACCMP200            Must be EOF                                  
         GOTOR FILTKEY,OLDFILE                                                  
         JL    ACCMP100            Filtered out key, get next record            
         JE    ACCMP110                                                         
         OI    OLDFILE,EOF         Force as if EOF                              
         J     ACCMP200                                                         
                                                                                
ACCMP110 GOTOR CHKREC,OLDFILE      See if special type                          
         JE    ACCMP200            This is normal record                        
*        GOTOR PROCREC,SORTOLD#                                                 
*        J     ACCMP100                                                         
                                                                                
*----------------------------------                                             
* Start processing new file now                                                 
*----------------------------------                                             
ACCMP200 CLI   CC,1                If low then OLD got new OLD rec              
         JL    ACCMP250            See if this one matches NEW record           
         GOTOR GETREC,NEWFILE                                                   
         JE    ACCMP210            Must be EOF                                  
         TM    OLDFILE,EOF                                                      
         JZ    ACCMP250                                                         
         J     ACCMP900            Done                                         
                                                                                
ACCMP210 GOTOR FILTKEY,NEWFILE                                                  
         JL    ACCMP200            Filtered out key get next                    
         JE    ACCMP220                                                         
         OI    NEWFILE,EOF         Force as if EOF                              
         TM    OLDFILE,EOF                                                      
         JO    ACCMP900            Done                                         
         J     ACCMP250                                                         
                                                                                
ACCMP220 GOTOR CHKREC,NEWFILE      See if special type                          
         JE    ACCMP250                                                         
*        GOTOR PUTREC,SORTNEW#                                                  
*        B     ACCMP200                                                         
                                                                                
ACCMP250 BRAS  RE,COMPARE          Compare OLD,NEW and print diffs              
         JE    ACCMP100            Matched keys matched                         
         JL    ACCMP100            Get old record                               
         J     ACCMP200            Get New record                               
                                                                                
ACCMP900 GOTOR CLOSEFIL,OLDFILE                                                 
         GOTOR CLOSEFIL,NEWFILE                                                 
         BRAS  RE,STATS                                                         
                                                                                
ACCMPXIT XBASE                                                                  
         EJECT ,                                                                
***********************************************************************         
* VALIDATE AGENCY CODE                                                *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VAGYC    MVC   ALPHA,SCANRHT       AGENCY=XX                                    
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT TYPE                                                *          
***********************************************************************         
         USING SCAND,R3                                                         
VINPUT1  NI    OLDFILE,ALL-(TAPE+DISK+RECOVERY)                                 
         LA    R2,OLDFILE                                                       
         B     VINPUT                                                           
                                                                                
VINPUT2  NI    NEWFILE,ALL-(TAPE+DISK+RECOVERY)                                 
         LA    R2,NEWFILE                                                       
         B     VINPUT                                                           
                                                                                
VINPUT   CLC   =C'TAPE',SCANRHT                                                 
         BNE   VINPUT10                                                         
         OI    0(R2),TAPE                                                       
         BR    RE                                                               
                                                                                
VINPUT10 CLC   =C'DISK',SCANRHT                                                 
         BNE   VINPUT20                                                         
         OI    0(R2),DISK                                                       
         TM    OLDFILE,DISK                                                     
         BZR   RE                                                               
         TM    NEWFILE,DISK        Can't have both as DISK                      
         BZR   RE                                                               
         DC    H'00'                                                            
                                                                                
VINPUT20 CLC   =C'RECOVERY',SCANRHT                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    0(R2),RECOVERY+TAPE                                              
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE START / END CARD                                           *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VUPSI    NTR1                                                                   
         CLI   SCANRLN,8           8 FOR BYTE OF BITS                           
         BE    *+6                                                              
         DC    H'00'               UPSI=00000000                                
                                                                                
         LA    R0,X'80'                                                         
         LA    R1,8                                                             
         SR    RF,RF                                                            
         LA    R2,SCANRHT          "00000000" PORTION                           
VUPSI10  CLI   0(R2),C'0'                                                       
         BE    VUPSI20                                                          
         CLI   0(R2),C'1'                                                       
         BE    *+6                                                              
         DC    H'00'               HAS TO BE 0 OR 1                             
         OR    RF,R0               OR ON BIT                                    
                                                                                
VUPSI20  SRL   R0,1                SHIFT BIT BY ONE                             
         AHI   R2,1                MOVE UP IN STRING                            
         BRCT  R1,VUPSI10                                                       
         STC   RF,UPSI                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE START / END CARD                                           *         
***********************************************************************         
                                                                                
VSTRC    NTR1  ,                                                                
         OI    INPUT,START         START=XXXXXXXX                               
         SR    RF,RF                                                            
         IC    RF,#OFKEYS                                                       
         AHI   RF,1                                                             
         CHI   RF,MAXKEYS                                                       
         BNH   *+6                                                              
         DC    H'00'                                                            
                                                                                
         STC   RF,#OFKEYS                                                       
         BCTR  RF,0                                                             
         MHI   RF,L'KEYSTRT                                                     
         LA    R4,KEYSTRT(RF)                                                   
         LA    R3,CARDIO+6                                                      
         B     VKEY                                                             
*                                                                               
VENDC    NTR1  ,                                                                
         OI    INPUT,END           END=XXXXXXXX                                 
         SR    RF,RF                                                            
         IC    RF,#OFKEYE                                                       
         AHI   RF,1                                                             
         CHI   RF,MAXKEYS          MAX NUMBER OF KEY PAIRS                      
         BNH   *+6                                                              
         DC    H'00'                                                            
                                                                                
         STC   RF,#OFKEYE                                                       
         CLC   #OFKEYS,#OFKEYE     MUST HAVE EQUAL #                            
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BCTR  RF,0                                                             
         MHI   RF,L'KEYEND                                                      
         LA    R4,KEYEND(RF)                                                    
         LA    R3,CARDIO+4                                                      
*                                                                               
VKEY     GOTO1 DECODE,DMCB,(42,(R3)),(X'00',(R4))                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,8(R1)                                                      
         GOTOR HEXOUT,DMCB,(R4),P+1,(RF),=C'TOG'                                
         BRAS  RE,ZPRINTNG                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE MAXDUMP                                                    *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VMXDMP   NTR1  ,                                                                
         OC    SCANRBV,SCANRBV     MAXDUMP=FULL WORD                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MAXDMP,SCANRBV                                                   
         B     XIT                                                              
***********************************************************************         
* BLOCK SIZE                                                                    
***********************************************************************         
VBLKSZ   NTR1  ,                                                                
         OC    SCANRBV,SCANRBV     MAXDUMP=FULL WORD                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   BLKSZ,SCANRBV                                                    
         B     XIT                                                              
***********************************************************************         
* RECORD LENGTH                                                                 
***********************************************************************         
VRECL    NTR1  ,                                                                
         OC    SCANRBV,SCANRBV     MAXDUMP=FULL WORD                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   RECLN,SCANRBV                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Ignore deletes?                                                               
***********************************************************************         
VDEL     NTR1  ,                                                                
         CLI   SCANRHT,C'Y'        DELETE=Y                                     
         BNE   XIT                                                              
         MVC   IGDEL,SCANRHT                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Set today as                                                                  
***********************************************************************         
VTODAY   NTR1  ,                                                                
         CLI   SCANRLN,6           TODAY=                                       
         JNL   *+6                                                              
         DC    H'00'                                                            
         GOTOR DATVAL,DMCB,(0,SCANRHT),TODAYMD                                  
         CLC   DATEYMD,=C'000000'                                               
         JNE   *+6                                                              
         DC    H'00'                                                            
         GOTOR DATCON,DMCB,TODAYMD,(1,TODAYP)                                   
         GOTOR DATCON,DMCB,TODAYMD,(2,TODAYC)                                   
         GOTOR DATCON,DMCB,TODAYMD,(3,TODAYB)                                   
         SR    RF,RF                                                            
         ICM   RF,3,TODAYC                                                      
         LCR   RF,RF                                                            
         BCTR  RF,0                                                             
         STCM  RF,3,TODAYX         2's complement compressed                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Date to replace with today as                                                 
***********************************************************************         
VDATEIS  NTR1  ,                                                                
         CLI   SCANRLN,6           DATE=                                        
         JNL   *+6                                                              
         DC    H'00'                                                            
         GOTOR DATVAL,DMCB,(0,SCANRHT),DATEYMD                                  
         CLC   DATEYMD,=C'000000'                                               
         JNE   *+6                                                              
         DC    H'00'                                                            
         GOTOR DATCON,DMCB,DATEYMD,(1,DATEP)                                    
         GOTOR DATCON,DMCB,DATEYMD,(2,DATEC)                                    
         GOTOR DATCON,DMCB,DATEYMD,(3,DATEB)                                    
         SR    RF,RF                                                            
         ICM   RF,3,DATEC                                                       
         LCR   RF,RF                                                            
         BCTR  RF,0                                                             
         STCM  RF,3,DATEX          2's complement compressed                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Show detialed differences                                                     
***********************************************************************         
VDETAIL  NTR1  ,                                                                
         CLI   SCANRLN,1           Must have at least length 1                  
         BNL   *+6                                                              
         DC    H'00'                                                            
         CLI   SCANRHT,YES         First character is a "Y", then YES           
         JNE   *+8                                                              
         MVI   SHOWDET,YES                                                      
         CLI   SCANRHT,C'R'        Show elements on records not found           
         JNE   *+8                                                              
         MVI   SHOWDET,C'R'        Record element details                       
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Initialize                                                                    
***********************************************************************         
INIT     NTR1                                                                   
         MVI   CC,1                This set it to equal                         
                                                                                
INIT1    GOTOR CARDS,DMCB,CARDIO,=C'RE00'                                       
         MVC   P(L'CARDIO),CARDIO                                               
         GOTOR PRINTER                                                          
         CLC   CARDIO(2),=C'/*'                                                 
         BE    INITX                                                            
         CLI   CARDIO,C'*'         Comment card                                 
         JE    INIT1               Next card                                    
         LA    R0,L'CARDIO                                                      
         LA    RF,CARDIO+L'CARDIO-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
                                                                                
         STC   R0,CARDH+5          SET LENGTH OF INPUT                          
         AHI   R0,L'CARDH                                                       
         STC   R0,CARDH                                                         
         L     R3,AIO1                                                          
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(40,CARDH),(10,SCAND),0                             
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          SET NUMBER OF PARAMETERS                     
         BNZ   *+6                                                              
         DC    H'0'                BAD CARD                                     
*                                                                               
INIT3    L     R2,=A(OPTTAB)       LIST OF VALID INPUT FIELDS                   
INIT5    LLC   R1,0(,R2)           LENGTH FOR COMPARE                           
         EXCLC R1,SCANLFT,1(R2)    MATCH CARD FIELD TO TABLE                    
         BE    INIT7                                                            
         LA    R2,L'OPTTAB(R2)                                                  
         CLI   0(R2),EOT                                                        
         BNE   INIT5                                                            
         DC    H'0'                INVALID INPUT OPTION                         
*                                                                               
INIT7    SR    RF,RF               GET VALIDATION ROUTINE                       
         ICM   RF,15,12(R2)                                                     
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         AHI   R3,SCANLNQ                                                       
         BCT   R0,INIT3                                                         
         B     INIT1               GET NEXT CARD                                
                                                                                
INITX    J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN CONTROL FILE AND GET SE NUMBER                                 *         
***********************************************************************         
OPENFIL  NTR1  ,                                                                
         MVC   BYTE,0(R1)                                                       
         TM    BYTE,TAPE+RECOVERY                                               
         JZ    OPENF10             No must be disk                              
         L     R3,ATAPEOLD                                                      
         TM    BYTE,INPOLD                                                      
         JO    *+8                                                              
         L     R3,ATAPENEW                                                      
*        MVC   62(L'BLKSZ,R3),BLKSZ                                             
*        MVC   82(L'RECLN,R3),RECLN                                             
         TM    BYTE,RECOVERY                                                    
         JO    OPENFIL2                                                         
         OPEN  ((3),(INPUT))                                                    
         J     XIT                                                              
                                                                                
OPENFIL2 OPEN  ((3),(OUTPUT))      Going to put recovery recs here              
                                                                                
         L     R3,ARCVOLD                                                       
         TM    BYTE,INPOLD                                                      
         JO    *+8                                                              
         L     R3,ARCVNEW                                                       
         OPEN  ((3),(INPUT))                                                    
         J     XIT                                                              
*                                                                               
         USING CT5REC,R2                                                        
OPENF10  TM    BYTE,DISK                                                        
         BO    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 DATAMGR,DMCB,OPEN,CONTROL,CTFILEL                                
         LA    R2,DKEY             READ ACCESS RECORD                           
         XC    DKEY,DKEY                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,ALPHA                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,AIO1                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
         USING CTSYSD,R3                                                        
OPENF12  CLI   0(R3),0             FIND SYSTEM ELEMENT                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTSYSELQ                                                   
         BNE   *+12                                                             
         CLI   CTSYSNUM,X'06'      TEST ACCOUNT FILE                            
         BE    OPENF14                                                          
         IC    R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     OPENF12                                                          
*                                                                               
OPENF14  MVC   SE,CTSYSSE          GET  SE NUMBER                               
         GOTO1 DATAMGR,DMCB,OPEN,ACCOUNT,ACFILEL                                
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Close file(s)                                                                 
***********************************************************************         
CLOSEFIL NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         TM    BYTE,DISK                                                        
         JO    CLOSEFX                                                          
         L     R3,ATAPEOLD                                                      
         TM    BYTE,INPOLD                                                      
         JO    *+8                                                              
         L     R3,ATAPENEW                                                      
         CLOSE (3)                                                              
                                                                                
         TM    BYTE,RECOVERY       Need to close recovery too ?                 
         JZ    CLOSEFX                                                          
         L     R3,ARCVOLD                                                       
         TM    BYTE,INPOLD                                                      
         JO    *+8                                                              
         L     R3,ARCVNEW                                                       
         CLOSE (3)                                                              
                                                                                
CLOSEFX  MVI   DMBYTE,0            Reset                                        
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* GET INPUT RECORD                                                    *         
***********************************************************************         
GETREC   NTR1  ,                                                                
         MVI   DMBYTE,0                                                         
         MVC   BYTE,0(R1)                                                       
         TM    0(R1),EOF                                                        
         JO    GETRECNO            Not equal                                    
         LR    R6,R1               R1=A(File indicators)                        
         L     R3,AIO1                                                          
         L     R2,ARCVOLD          A(DCB for OLD recovery)                      
         TM    BYTE,INPOLD                                                      
         JO    *+8                                                              
         L     R2,ARCVNEW          A(DCB for OLD recovery)                      
         TM    BYTE,RECOVERY                                                    
         JO    GETREC02            Get recovery records                         
                                                                                
         L     R2,ATAPEOLD         A(DCB for OLD tape)                          
         L     R3,AOLDFILE         A(Input for old file)                        
         TM    BYTE,INPOLD                                                      
         JO    GETREC02                                                         
         L     R2,ATAPENEW                                                      
         L     R3,ANEWFILE         A(Input for new file)                        
                                                                                
GETREC02 TM    BYTE,DISK                                                        
         JO    GETREC10                                                         
         SHI   R3,4                Length of length field                       
         GET   (2),(3)                                                          
*** IDF                                                                         
         CLC   =X'370C',4(R3)                                                   
         BNE   *+6                                                              
         LTR   R4,R4                                                            
***                                                                             
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)                                                       
         AR    R4,R3                                                            
         XC    0(8,R4),0(R4)       Clear end of record                          
         J     GETRECOK                                                         
                                                                                
***********************************************************************         
* Process record from DISK                                                      
***********************************************************************         
GETREC10 TM    INPUT,NEXT          TEST FIRST TIME                              
         BO    GETREC15                                                         
         MVC   DKEY,KEYSTRT                                                     
         GOTO1 ADMGR,DMHI                                                       
         OI    INPUT,NEXT                                                       
         B     GETREC20                                                         
*                                                                               
GETREC15 GOTO1 ADMGR,DMSEQ                                                      
         TM    DMBYTE,X'80'        TEST EOF                                     
         JO    GETIEOF                                                          
                                                                                
GETREC20 GOTO1 ADMGR,DMGET         GET THE RECORD                               
         OI    IS_DATA,IS_PASS     Set to passive                               
         CLC   0(42,R2),DIR        TEST RECORD = KEY                            
         JNE   GETREC15            MUST BE PASSIVE                              
         NI    IS_DATA,TURNOFF-IS_PASS                                          
         SR    RE,RE                                                            
         ICM   RE,3,ACCRLEN-ACCRECD(R2)                                         
         AHI   RE,4                SET LENGTH                                   
         LR    RF,R2                                                            
         SHI   RF,4                                                             
         STCM  RE,3,0(RF)          Save off length of record                    
         J     GETRECOK                                                         
                                                                                
GETIEOF  OI    0(R6),EOF           Set End of file (DCB jumps here)             
         J     GETRECNO                                                         
                                                                                
GETRECOK LA    RE,TOT#OLD                                                       
         TM    0(R6),INPOLD        Old input                                    
         JO    *+8                 Yes Old                                      
         LA    RE,TOT#NEW          No  New                                      
         AP    0(L'TOT#OLD,RE),=P'1'                                            
                                                                                
         SR    RE,RE                                                            
GETRECNO LTR   RE,RE                                                            
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET INPUT RECORD                                                    *         
***********************************************************************         
PUTREC   NTR1  ,                                                                
         L     R2,ATAPEOLD         A(DCB for OLD tape)                          
         L     R3,AOLDFILE         A(Input for old file)                        
         TM    0(R1),INPOLD                                                     
         JO    PUTREC10                                                         
         L     R2,ATAPENEW                                                      
         L     R3,ANEWFILE         A(Input for new file)                        
                                                                                
         USING ACCRECD,R3                                                       
PUTREC10 SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN                                                     
         AHI   RF,4                                                             
         DROP  R3                                                               
                                                                                
         SHI   R3,4                Length of length field                       
         STH   RF,0(,R3)           Save length of record                        
         PUT   (2),(3)                                                          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* See if this record type as special rules                                      
***********************************************************************         
CHKREC   NTR1                                                                   
         J     XIT                                                              
*                                                                               
         LR    R6,R1               Save off A(parameter) passed                 
         L     R5,AOLDFILE                                                      
         TM    0(R1),INPOLD                                                     
         JO    *+8                                                              
         L     R5,ANEWFILE                                                      
         GOTOR ACRECTYP,DMCB,(C'D',(R5))                                        
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
         LA    RE,RECTYOLD                                                      
         LA    R1,ISOLD                                                         
         TM    0(R6),INPOLD                                                     
         JO    CHKREC05                                                         
         LA    R1,ISNEW                                                         
         LA    RE,RECTYNEW                                                      
                                                                                
CHKREC05 MVC   0(1,RE),RECTYPE                                                  
         GOTOR RECRPT                                                           
         CLI   SHOWDET,YES         Show all element differences                 
         JE    CHKREC10                                                         
         GOTOR CLEARIT,0(R6)       Clean out element that won't match           
                                                                                
CHKREC10 LLC   RE,RECTYPE                                                       
         BCTR  RE,0                                                             
         SLL   RE,1                Multiply by 2                                
         A     RE,=A(RECLIST)                                                   
         OC    0(2,RE),0(RE)       Has rules? Set condition code                
         J     XIT                                                              
         EJECT ,                                                                
*&&DO                                                                           
***********************************************************************         
* Put record to sorter                                                          
***********************************************************************         
         USING SORTD,R4                                                         
PUTREC   NTR1                                                                   
         L     R4,ASORT                                                         
         STC   R1,SORTSEQ#                                                      
         TM    SORTSW,SORTINT                                                   
         BO    PUTREC10                                                         
         GOTOR SORTER,DMCB,SORTCARD,RECCARD,0                                   
         OI    SORTSW,SORTINT                                                   
                                                                                
         USING ACCRECD,R2                                                       
PUTREC10 L     R2,AIO1                                                          
         LA    R3,ACCRFST                                                       
         GOTOR ACRECTYP,DMCB,(C'D',(R2))                                        
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
         BRAS  RE,BLDKEY                                                        
         MVC   SORTKEY,0(R2)       Set the key                                  
         BRAS  RE,FIXELMS                                                       
                                                                                
         LA    RE,SORTDATA                                                      
         ICM   RF,3,ACCRLEN                                                     
         SHI   RF,ACCRFST-ACCRECD                                               
         LR    R2,RF                                                            
         AHI   R2,SORTKLNQ                                                      
         LR    R1,RF                                                            
         LR    R0,R3                                                            
         MVCL  RE,R0                                                            
         L     RE,ASORTL           length of sort record                        
         ST    R2,0(,RE)                                                        
                                                                                
PUTREC80 GOTOR SORTER,DMCB,=C'PUT',ASORTL                                       
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* Build key for sorter                                                          
***********************************************************************         
BLDKEY   NTR1                                                                   
         XC    SORTKXT,SORTKXT     Extra sort key                               
         LLC   RE,RECTYPE                                                       
         BCTR  RE,0                                                             
         MHI   RE,2                                                             
         A     RE,=A(RECLIST)                                                   
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          Get displacment to table                     
         BZ    BLDKEYX                                                          
         A     RF,=A(RECKEYS)                                                   
                                                                                
BLDKEY10 CLI   0(RF),EOT                                                        
         BE    BLDKEYX                                                          
         CLI   0(RF),RECKCLR       Clear area in key                            
         BNE   BLDKEY20                                                         
         ICM   R6,3,2(RF)                                                       
         AR    R6,R2               R2 = Base of key                             
         LLC   R1,1(RF)            Length to clear for                          
         BCTR  R1,0                                                             
         EXXC  R1,0(R6),0(R6)                                                   
         B     BLDKEY80                                                         
                                                                                
BLDKEY20 CLI   0(RF),RECKXTR       Extra data to put in sortkey                 
         BE    *+6                                                              
         DC    H'00'                                                            
         LR    R6,R3               R3 is start of elements                      
                                                                                
BLDKEY22 CLI   0(R6),EOR           End of record                                
         BNE   *+6                                                              
         DC    H'00'               For now must have element                    
         CLC   0(1,R6),1(RF)       Match on element code                        
         BE    BLDKEY30                                                         
         LLC   R1,1(,R6)           Length of element                            
         AR    R6,R1                                                            
         B     BLDKEY22                                                         
                                                                                
BLDKEY30 LLC   R1,3(,RF)           Displacement into element                    
         AR    R6,R1                                                            
         LLC   R1,2(,RF)           Length to move into SORTKXT                  
         BCTR  R1,0                                                             
         EXMVC R1,SORTKXT,0(R6)                                                 
                                                                                
BLDKEY80 AHI   RF,4                                                             
         B     BLDKEY10                                                         
                                                                                
BLDKEYX  J     XIT                                                              
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
* Fix records  that will cause differences                                      
* Fix elements that will cause differences                                      
***********************************************************************         
CLEARIT  NTR1                                                                   
         LR    R6,R1               Save off A(parameter)                        
         L     R3,AOLDFILE                                                      
         TM    0(R1),INPOLD                                                     
         JO    *+8                                                              
         L     R3,ANEWFILE                                                      
                                                                                
         SR    R2,R2                                                            
         ICM   R2,1,RECTYPE        Get record type                              
         JZ    CLRIT200            None, or unknown                             
         SLL   R2,1                Index int RECLIST                            
         A     R2,=A(RECLIST)                                                   
                                                                                
         USING RECTABD,R4                                                       
         SR    R4,R4                                                            
         ICM   R4,3,0(R2)          Displacement to entry                        
         JZ    CLRIT200            Nothing defined                              
         CLI   RECTYPE,ACRTBAT                                                  
         JNE   *+8                                                              
         OILH  GRF,X'8000'                                                      
         A     R4,=A(RECTAB)       Base of record table                         
CLRIT110 CLI   0(R4),EOT           End of table?                                
         JE    CLRIT200            Yes                                          
         LLC   RF,RCCLRLEN                                                      
         SR    RE,RE                                                            
         ICM   RE,3,RCCLRDSP                                                    
         AR    RE,R3               A(Data in key). R3=Start of key              
         BCTR  RF,0                                                             
         TM    RCCLRIND,RCCLRD2T   Match date and replace                       
         JO    CLRIT120                                                         
         XC    0(0,RE),0(RE)                                                    
         EX    RF,*-6                                                           
         J     CLRIT130                                                         
                                                                                
CLRIT120 LLC   R1,RCCLRDTE                                                      
         LA    R7,DATEDSP(R1)      Point to displacement to date                
         LLC   R1,0(,R7)           Get displacement                             
         LA    R7,DATEYMD(R1)      Point to date we want                        
         CLC   0(0,RE),0(R7)                                                    
         EX    RF,*-6                                                           
         JNE   CLRIT130            Keep it as is for now                        
         LA    R7,TODAYMD(R1)      Point to date we want                        
         MVC   0(0,RE),0(R7)                                                    
         EX    RF,*-6              Move in new today date                       
                                                                                
CLRIT130 LA    R4,RECLNQ(,R4)                                                   
         J     CLRIT110                                                         
         DROP  R4                                                               
                                                                                
CLRIT200 AH    R3,KEYLEN           R3=A(Start of elements)                      
CLRIT210 SR    R2,R2                                                            
         ICM   R2,1,0(R3)          Get element code                             
         JZ    CLRITXIT            If zero, the EOR                             
         SLL   R2,2                Index into ELMLIST                           
         A     R2,=A(ELMLIST)      Base of list of elements                     
                                                                                
         USING ELMTABD,R4                                                       
         SR    R4,R4                                                            
         ICM   R4,3,0(R2)          Displacement to entry                        
         JZ    CLRIT290            No element to modifiy when zero              
         A     R4,=A(ELMTAB)       Base of element table                        
CLRIT220 CLI   0(R4),EOT           End of table                                 
         JE    CLRIT290            Done for this element                        
         CLI   ELCLRTYP,0          Need special record type                     
         JE    CLRIT230            No                                           
         CLC   ELCLRTYP,RECTYPE    RECTYPE set in CHKREC                        
         JNE   CLRIT260            Not rigth one, so skip                       
                                                                                
CLRIT230 CLI   ELMTYPE,0           Element type                                 
         JE    CLRIT240            No                                           
         LLH   RE,ELMTYDSP                                                      
         CLM   RE,1,1(R3)          Is displacment > element                     
         JH    CLRIT260            Yes so can't clear it                        
         AR    RE,R3               R3 = Start of element                        
         CLC   0(1,RE),ELMTYPE                                                  
         JNE   CLRIT260                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,ELMRTE#                                                     
         JZ    CLRIT240                                                         
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     ELMROUT1                                                         
*        B     ELMROUT2                                                         
*                                                                               
CLRIT240 LLC   RF,ELCLRLEN         Length to clear for                          
         SR    RE,RE                                                            
         CLC   1(1,R3),ELCLRDSP+1                                               
         JNH   CLRIT260            This is past end of element                  
         ICM   RE,3,ELCLRDSP       Displacement into element                    
         AR    RE,R3               R3 = start of element                        
                                                                                
CLRIT248 BCTR  RF,0                RF = length to clear                         
         XC    0(0,RE),0(RE)                                                    
         EX    RF,*-6                                                           
                                                                                
CLRIT260 LA    R4,ELMLNQ(,R4)      Next entry                                   
         J     CLRIT220                                                         
         DROP  R4                                                               
                                                                                
CLRIT290 LLC   RF,1(,R3)           Length to next element                       
         AR    R3,RF                                                            
         B     CLRIT210                                                         
                                                                                
CLRITXIT J     XIT                                                              
         EJECT ,                                                                
         USING ELMTABD,R4                                                       
ELMROUT1 LLC   RF,ELCLRLEN         Length to clear for                          
         SR    RE,RE                                                            
         CLC   1(1,R3),ELCLRDSP+1                                               
         JNH   CLRIT260            This is past end of element                  
         LLC   RE,1(,R3)           lenght of element                            
         AR    RE,R3               R3 = start of element                        
         SR    RE,RF                                                            
         J     CLRIT248                                                         
         DROP  R4                                                               
         EJECT ,                                                                
*&&DO                                                                           
***********************************************************************         
* Create sort record for Transaction                                            
***********************************************************************         
         USING TRNRECD,R2                                                       
TRNREC   DS    0H                                                               
         MVI   TRNKSBR,0           Clear out sequence                           
         MVC   SORTXTRA(TRNLN1Q-TRNTYPE),TRNTYPE                                
         B     RETURNX                                                          
         DROP  R2                                                               
                                                                                
RETURNX  B     PUTREC60                                                         
*&&                                                                             
***********************************************************************         
* Filter out records based on start and end keys                                
***********************************************************************         
FILTKEY  NTR1                                                                   
         J     FILTYES                                                          
*                                                                               
         LR    R6,R1                                                            
         L     R2,AOLDFILE         A(Record)                                    
         TM    0(R6),INPOLD                                                     
         JO    *+8                                                              
         L     R2,ANEWFILE                                                      
         CLI   IGDEL,YES           Ignore deleted records                       
         JNE   FILTKY02                                                         
         LR    RE,R2                                                            
         AH    RE,KEYDEL                                                        
         TM    0(RE),X'80'                                                      
         JO    FILTNO              Ignore key                                   
                                                                                
FILTKY02 CLI   #OFKEYS,1           # of keys to compare against                 
         JE    FILTKY50                                                         
*---------------------*                                                         
* Multiple keys pairs *                                                         
*---------------------*                                                         
         LA    R1,1                                                             
FILTKY10 LR    RF,R1                                                            
         BCTR  RF,0                                                             
         MHI   RF,L'KEYSTRT                                                     
         LA    RE,KEYSTRT(RF)                                                   
         CLC   0(L'KEYSTRT,R2),0(RE)                                            
         JL    FILTKY20                                                         
                                                                                
         LA    RE,KEYEND(RF)                                                    
         CLC   0(L'KEYEND,R2),0(RE)                                             
         JNH   FILTYES             WAS VALID KEY                                
         CLM   R1,1,#OFKEYE                                                     
         JNL   FILTDONE            DONE                                         
                                                                                
FILTKY20 AHI   R1,1                                                             
         CLM   R1,1,#OFKEYS                                                     
         JH    FILTNO              GET NEXT RECORD                              
         J     FILTKY10            TRY NEXT PAIR OF KEYS                        
*--------------------------*                                                    
* SINGLE START AND/OR END  *                                                    
*--------------------------*                                                    
FILTKY50 TM    INPUT,START         TEST START=XXX                               
         BZ    FILTKY52                                                         
         CLC   0(L'KEYSTRT,R2),KEYSTRT                                          
         BL    FILTNO                                                           
                                                                                
FILTKY52 TM    INPUT,END           TEST END=XXX                                 
         BZ    FILTYES                                                          
         CLC   0(L'KEYEND,R2),KEYEND                                            
         BH    FILTDONE                                                         
                                                                                
FILTYES  LA    R1,1                Set for equal                                
         LA    RE,REC#OLD                                                       
         TM    0(R6),INPOLD        OLD input ?                                  
         JO    *+8                 Yes                                          
         LA    RE,REC#NEW          No, new                                      
         AP    0(L'REC#OLD,RE),=P'1'                                            
         J     FILTXIT                                                          
                                                                                
FILTDONE LA    R1,2                Set for high                                 
         J     FILTXIT                                                          
                                                                                
FILTNO   SR    R1,R1               Set for low                                  
                                                                                
FILTXIT  CHI   R1,1                                                             
         J     XIT                                                              
***********************************************************************         
* Print key of record because found on one but not the other                    
***********************************************************************         
                                                                                
PRTREC   NTR1  ,                                                                
         MVC   BYTE,0(R1)                                                       
         MVC   SVTYP,RECTYOLD                                                   
         LA    R2,#OFOLD                                                        
         MVC   WORK(7),=C'**OLD**'                                              
         L     R7,AOLDFILE                                                      
         TM    BYTE,INPOLD         Input 1 is old                               
         JO    PRTREC10            Yes                                          
         MVC   SVTYP,RECTYNEW                                                   
         LA    R2,#OFNEW           No, New                                      
         MVC   WORK(7),=C'**NEW**'                                              
         L     R7,ANEWFILE                                                      
                                                                                
PRTREC10 GOTOR RPTTYPE,SVTYP                                                    
         AP    0(L'#OFOLD,R2),=P'1'                                             
         ST    R7,FULL                                                          
         MVC   P+1(7),WORK                                                      
         GOTOR HEXOUT,DMCB,(R7),P+8,HEXPLNQ,=C'TOG'                             
         MVC   P+(HEXPLNQ*2+10)(HEXPLNQ),0(R7)                                  
         BRAS  RE,ZPRINTNG                                                      
         AHI   R7,HEXPLNQ                                                       
         GOTOR HEXOUT,DMCB,(R7),P+8,ACCRFST-ACCRECD-HEXPLNQ,=C'TOG'             
         MVC   P+(HEXPLNQ*2+10)(ACCRFST-ACCRECD-HEXPLNQ),0(R7)                  
         BRAS  RE,ZPRINTNG                                                      
         CLI   SHOWDET,C'R'                                                     
         JNE   PRTREC30                                                         
         L     R5,FULL             Set to print elements                        
         AH    R5,KEYLEN                                                        
                                                                                
PRTREC20 CLI   0(R5),EOR                                                        
         JE    PRTREC30                                                         
         BRAS  RE,PRTELM                                                        
         LLC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         J     PRTREC20                                                         
                                                                                
PRTREC30 BRAS  RE,ZPRINTNG         Print blank line                             
                                                                                
PDUMPX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Basic compare of old record to new record, print differences                  
***********************************************************************         
COMPARE  NTR1                                                                   
         L     R2,AOLDFILE                                                      
         L     R3,ANEWFILE                                                      
         MVI   CC,2                To set cc=high. Get next new                 
         MVI   DIFFSTAT,NO                                                      
         TM    OLDFILE,EOF         Are we at end of old file                    
         JO    COMP910             Yes, so print new file                       
         MVI   CC,0                To set cc=low. Get next old                  
         TM    NEWFILE,EOF         Are we at of new file                        
         JO    COMP900             Yes, so print old file                       
         LH    R1,0(R2)                                                         
         BCTR  R1,0                                                             
         CLC   0(0,R2),0(R3)       Old to New                                   
         EX    R1,*-6                                                           
         MVI   CC,0                To set cc=low. Get next old                  
         JL    COMP900                                                          
         MVI   CC,2                To set cc=high. Get next new                 
         JH    COMP910                                                          
         MVI   CC,1                Always set equal since keys matched          
         J     COMPXIT                                                          
                                                                                
COMP900  GOTOR PRTREC,OLDFILE                                                   
         GOTOR RECRPT,ISDIFKEY                                                  
         J     COMPXIT                                                          
                                                                                
COMP910  GOTOR PRTREC,NEWFILE                                                   
         GOTOR RECRPT,ISDIFKEY                                                  
                                                                                
COMPXIT  CLI   CC,1                                                             
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* Compare and print differences with elements                                   
***********************************************************************         
ELMATCH  ST    RE,SVRE                                                          
         MVI   ANYDIFF,NO          Set to no differences                        
                                                                                
ELMAT100 CLC   0(1,R2),0(R3)       Same element or EOF                          
         JNE   ELMAT200            Not same                                     
         CLI   0(R2),EOR           End or records?                              
         JE    ELMATXIT            Yes                                          
         CLC   1(1,R2),1(R3)       Length the same ?                            
         JNE   ELMAT200            No                                           
         LLC   RF,1(,R2)           Get length of old element                    
         BCTR  RF,0                                                             
         CLC   0(0,R2),0(R3)       Compare elements                             
         EX    RF,*-6                                                           
         JNE   ELMAT200            Diffrent                                     
         LA    R2,1(RF,R2)         Next old element                             
         LA    R3,1(RF,R3)         Next new element                             
         J     ELMAT100                                                         
*                                                                               
* Process old elements first                                                    
*                                                                               
ELMAT200 MVI   ANYDIFF,YES         Set, found differences                       
         GOTOR RPTTYPE,RECTYPE                                                  
         MVC   P+1(7),=CL7'**REC**'                                             
         L     R7,AOLDFILE                                                      
         GOTOR HEXOUT,DMCB,(R7),P+8,HEXPLNQ,=C'TOG'                             
         MVC   P+(HEXPLNQ*2+10)(HEXPLNQ),0(R7)                                  
         BRAS  RE,ZPRINTNG                                                      
         AHI   R7,HEXPLNQ                                                       
         GOTOR HEXOUT,DMCB,(R7),P+8,ACCRFST-ACCRECD-HEXPLNQ,=C'TOG'             
         MVC   P+(HEXPLNQ*2+10)(ACCRFST-ACCRECD-HEXPLNQ),0(R7)                  
         BRAS  RE,ZPRINTNG                                                      
                                                                                
ELMAT210 LR    R6,R3               R3 = New record elements                     
         MVI   ONLYONE,YES         Only one of this type?                       
ELMAT220 CLI   0(R2),EOR           End of old record ?                          
         JE    ELMAT300            Yes, done, now do NEW                        
         CLI   0(R6),EOR           End of new record ?                          
         JNE   ELMAT226            Yes print OLD element                        
         LR    R5,R2               Set to print old element                     
         BRAS  RE,PRTELM                                                        
         MVI   0(R2),X'FF'         Mark done                                    
         J     ELMAT250                                                         
                                                                                
ELMAT226 CLI   0(R6),X'FF'         Already processed                            
         JE    ELMAT230            Next element                                 
         CLC   0(1,R2),0(R6)       Look in new record. Same element             
*        JNE   ELMAT230            Different                                    
*        CLC   1(1,R2),1(R6)       Match lengths                                
         JE    ELMAT240            Maybe a match                                
ELMAT230 LLC   RF,1(,R6)                                                        
         AR    R6,RF                                                            
         J     ELMAT220            Look at next element                         
                                                                                
                                                                                
ELMAT240 LLC   RF,1(,R2)                                                        
         CLC   1(1,R2),1(R6)       Lengths the same?                            
         JNE   ELMAT241            No                                           
         BCTR  RF,0                                                             
         CLC   0(1,R2),0(R6)                                                    
         EX    RF,*-6                                                           
         JE    ELMAT242            Found no differences                         
                                                                                
ELMAT241 GOTOR SHOWDIFF,DMCB,(R2),(R6)    Display element differences           
                                                                                
ELMAT242 MVI   0(R2),X'FF'         Mark done                                    
         MVI   0(R6),X'FF'         Mark done                                    
                                                                                
ELMAT250 LLC   RF,1(,R2)           See if you can find element                  
         AR    R2,RF                                                            
         J     ELMAT210                                                         
                                                                                
ELMAT300 CLI   0(R3),EOR           Done with NEW record                         
         JE    ELMATXIT            Yes, print both                              
         CLI   0(R3),X'FF'         Already processed                            
         JE    ELMAT320                                                         
         LR    R5,R3               Set to print New element                     
         BRAS  RE,PRTELM                                                        
                                                                                
ELMAT320 LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     ELMAT300            Find next                                    
                                                                                
ELMATXIT LHI   R1,ISOKAY           Matched                                      
         CLI   ANYDIFF,NO                                                       
         JE    *+8                                                              
         LHI   R1,ISDIFELM         Different                                    
         GOTOR RECRPT                                                           
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Print out element at R5                                                       
***********************************************************************         
PRTELM   NTR1                                                                   
         MVC   P+1(7),=C'--OLD--'                                               
         CR    R5,R2                                                            
         JE    *+10                                                             
         MVC   P+1(7),=C'--NEW--'                                               
         LLC   R6,1(,R5)           Length of element                            
PRTEL10  LR    R4,R6               Figure out how much to print                 
         SHI   R6,HEXPLNQ          Max that fits on a line                      
         JNP   *+8                                                              
         LHI   R4,HEXPLNQ                                                       
         GOTOR HEXOUT,DMCB,(R5),P+8,(R4),=C'TOG'                                
         BCTR  R4,0                                                             
         MVC   P+(HEXPLNQ*2+10)(0),0(R5)                                        
         EX    R4,*-6                                                           
         AHI   R4,1                Add back  one for EX instr.                  
         BRAS  RE,ZPRINTNG                                                      
         AR    R5,R4                                                            
         LTR   R6,R6                                                            
         JP    PRTEL10                                                          
         BRAS  RE,ZPRINTNG         Print blank line                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Highlight differences                                                         
* P1 = OLD element, P2 = New element                                            
***********************************************************************         
SHOWDIFF NTR1                                                                   
         L     R2,0(,R1)           Old element                                  
         L     R3,4(,R1)           New element                                  
         LLC   R5,1(,R2)           Length of old element                        
         LLC   R6,1(,R3)           Length of new element                        
         MVI   DOIT,DO1+DO2                                                     
*------------------------*                                                      
* Now print Old element  *                                                      
*------------------------*                                                      
SHOWD10  LTR   R4,R5               Figure out how much to print                 
         JP    SHOWD12             Keep doing it                                
         NI    DOIT,TURNOFF-DO1    Okay stop printing out element               
         J     SHOWD20                                                          
                                                                                
SHOWD12  SHI   R5,HEXPLNQ          Max that fits on a line                      
         JNP   *+8                                                              
         LHI   R4,HEXPLNQ                                                       
         MVC   P+1(7),=C'--OLD--'                                               
         GOTOR HEXOUT,DMCB,(R2),P+8,(R4),=C'TOG'                                
         BCTR  R4,0                                                             
         MVC   P+(HEXPLNQ*2+10)(0),0(R2)                                        
         EX    R4,*-6                                                           
         AHI   R4,1                Add back  one for EX instr.                  
         BRAS  RE,ZPRINTNG                                                      
         ST    R4,PRINTED1                                                      
         LR    R0,R4               Save off how much we printed                 
*------------------------*                                                      
* Now print New element  *                                                      
*------------------------*                                                      
SHOWD20  LTR   R4,R6               Figure out how much to print                 
         JP    SHOWD22             Keep doing it                                
         NI    DOIT,TURNOFF-DO2    Okay stop printing out element               
         J     SHOWD40                                                          
                                                                                
SHOWD22  SHI   R6,HEXPLNQ          Max that fits on a line                      
         JNP   *+8                                                              
         LHI   R4,HEXPLNQ                                                       
         MVC   P+1(7),=C'--NEW--'                                               
         GOTOR HEXOUT,DMCB,(R3),P+8,(R4),=C'TOG'                                
         BCTR  R4,0                                                             
         MVC   P+(HEXPLNQ*2+10)(0),0(R3)                                        
         EX    R4,*-6                                                           
         AHI   R4,1                Add back  one for EX instr.                  
         BRAS  RE,ZPRINTNG                                                      
         ST    R4,PRINTED2                                                      
*------------------------------------------------*                              
* Now print nice little stars under the changes  *                              
*------------------------------------------------*                              
SHOWD30  LA    RE,P+8              HEX out area                                 
         LA    RF,P+(HEXPLNQ*2+10) MVC out area                                 
         CR    R4,R0                                                            
         JNH   *+6                 Use R4 (length printed new elem)             
         LR    R4,R0               Else R0 length printed old elem              
         LR    R1,R2               R1=Old                                       
         LR    R7,R3               R7=New                                       
SHOWD32  CLC   0(1,R1),0(R7)       compare each part of element                 
         JE    SHOWD38                                                          
         MVC   0(2,RE),=C'**'                                                   
         MVI   0(RF),C'*'                                                       
SHOWD38  AHI   RE,2                Next hex out pair                            
         AHI   RF,1                Next character                               
         LA    R1,1(,R1)                                                        
         LA    R7,1(,R7)                                                        
         BRCT  R4,SHOWD32                                                       
         BRAS  RE,ZPRINTNG         The printing                                 
                                                                                
SHOWD40  TM    DOIT,DO1+DO2        Next part to print in old                    
         JZ    SHOWDXIT            Show dum the exit                            
         TM    DOIT,DO1                                                         
         JZ    *+8                                                              
         A     R2,PRINTED1         Next part to print in new                    
         TM    DOIT,DO2                                                         
         JZ    *+8                                                              
         A     R3,PRINTED2         Next part to print in new                    
         J     SHOWD10                                                          
                                                                                
SHOWDXIT BRAS  RE,ZPRINTNG         Print a blank line                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* The print of some output                                                      
***********************************************************************         
ZPRINTNG ST    RE,SVZRE                                                         
         GOTOR =V(PRINT),DMCB,P,SPACING                                         
         MVC   P,SPACES                                                         
         L     RE,SVZRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Suck in recovery files and create what looks like mini dump files             
***********************************************************************         
RCV2DMP  NTR1                                                                   
         LR    R6,R1               Save R1 in R6                                
         L     R5,AOLDFILE                                                      
         TM    0(R6),INPOLD        Old file ?                                   
         JO    *+8                 Yes                                          
         L     R5,ANEWFILE         No, new file                                 
                                                                                
         GOTOR BUFFRIN,DMCB,('BUFFAINI',KEYBUFF),AIO1,COMFACS                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
RCV2D100 GOTOR GETREC,(R6)         Get recovery record                          
         TM    0(R6),EOF           End of file                                  
         JO    RCV2D200            Done                                         
                                                                                
         USING RECVHDR,R3                                                       
         USING ACCRECD,R4                                                       
         L     R3,AIO1             Recovery read to IO1                         
         CLI   RTASKID,X'FF'       Unwind from dumps                            
         JE    RCV2D100            Skip these                                   
         LLC   R0,#OFFILES         Files to examine on recovery                 
         LA    RE,FILELIST                                                      
RCV2D108 CLC   RFILTY,0(RE)        Keep this record ?                           
         JE    RCV2D110                                                         
         LA    RE,1(,RE)                                                        
         BRCT  R0,RCV2D108                                                      
         J     RCV2D100            Skip, not in list                            
                                                                                
RCV2D110 LR    R4,R3                                                            
         AHI   R4,L'RECVHDR        Point to record                              
         CLI   RRECTY,RCVCPY       Copy ?                                       
         JE    RCV2D100            Skip these                                   
                                                                                
         LR    RE,R4               Move to old or new file area                 
         SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN                                                     
         LHI   R1,L'IO1                                                         
         LR    R0,R5               R5 has AOLDFILE or ANEWFILE                  
         MVCL  R0,RE                                                            
                                                                                
         GOTOR FILTKEY,(R6)        Filter out keys we don't want                
         JNE   RCV2D100            Get next, records not in order so            
*                                    can only do JNE not JL and JH              
         CLI   RRECTY,RCVCHG       If have then replace, else add               
         JE    RCV2D120            Yes action change                            
         CLI   RRECTY,RCVADD       Add ?                                        
         JNE   RCV2D100            No, probably x'81' or x'82' get next         
         DROP  R3,R4                                                            
                                                                                
RCV2D120 GOTOR BUFFRIN,DMCB,('BUFFAPUT',KEYBUFF),(R5),COMFACS                   
         JE    RCV2D100            Get next                                     
         DC    H'0'                                                             
                                                                                
*-------------------------------------------*                                   
* Close recovery and output dump type tape  *                                   
*-------------------------------------------*                                   
RCV2D200 GOTOR CLOSEFIL,(R6)                                                    
         XC    0(100,R5),0(R5)                                                  
         MVI   BUFFACT,BUFFARDH    Read first record                            
RCV2D210 GOTOR BUFFRIN,DMCB,(BUFFACT,KEYBUFF),(R5),COMFACS                      
         JNE   RCV2DXIT                                                         
         MVI   BUFFACT,BUFFASEQ    Read squencial                               
         GOTOR PUTREC,(R6)                                                      
         J     RCV2D210                                                         
                                                                                
RCV2DXIT NI    0(R6),TURNOFF-RECOVERY   Done with this recovery                 
         GOTOR CLOSEFIL,(R6)            Close Tape so we can read again         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Print out statistics                                                          
***********************************************************************         
         USING STATD,R2                                                         
STATS    NTR1                                                                   
                                                                                
         L     R2,=A(STATTAB)                                                   
STATS10  CLI   0(R2),EOT           End of table ?                               
         JE    STATS20                                                          
         MVC   P+1(L'STATTXT),STATTXT                                           
         MVI   P+22,C'='                                                        
         L     R3,STATAMT                                                       
         OI    L'MATCHED-1(R3),X'0F'                                            
         UNPK  P+23(10),0(L'MATCHED,R3)                                         
         BRAS  RE,ZPRINTNG                                                      
         LA    R2,STATLNQ(,R2)                                                  
         J     STATS10                                                          
                                                                                
STATS20  GOTOR RECRPT,ISREPORT                                                  
                                                                                
STATXIT  J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                   *         
***********************************************************************         
                                                                                
DMGR     NTR1  ,                                                                
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     DMGREAD                                                          
         B     DMGHIGH                                                          
         B     DMGSEQ                                                           
         B     DMGGET                                                           
*                                                                               
DMGREAD  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGHIGH  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGSEQ   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGGET   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMGETR,ACCMST,DA,(R2),DMWORK                        
*                                                                               
DMERR    MVC   DMBYTE,8(R1)                                                     
         NI    DMBYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED             
         TM    DMBYTE,X'80'        PASS BACK EOF                                
         BO    XIT                                                              
         CLI   DMBYTE,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Print report type                                                             
***********************************************************************         
         USING TYPD,R5                                                          
RPTTYPE  NTR1                                                                   
         LLC   R5,0(,R1)           R1 = A(Report type)                          
         GOTOR FINDTYP,(R5)                                                     
         LR    R5,R1                                                            
RPTTYP80 MVC   P,SPACES                                                         
         MVC   P+1(L'TYPNAME),TYPNAME                                           
         BRAS  RE,ZPRINTNG                                                      
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* R1 = record type number, Return R1 = A(Table entry in ACTYPTAB)               
***********************************************************************         
         USING TYPD,R5                                                          
FINDTYP  NTR1                                                                   
         LR    R5,R1                                                            
         MHI   R5,TYPLNQ                                                        
         A     R5,=A(ACTYPTAB)     Point to table entry                         
         CLM   R1,1,TYPCODE        Did we get a hit                             
         BE    FINDTY80            Yes                                          
         L     R5,=A(ACTYPTAB)     Set to unknown                               
FINDTY10 CLI   0(R5),EOT           No, so table not rigth. End of table         
         BE    FINDTY70            Yes                                          
         CLM   R1,1,TYPCODE                                                     
         BE    FINDTY80                                                         
         AHI   R5,TYPLNQ                                                        
         B     FINDTY10                                                         
                                                                                
FINDTY70 L     R5,=A(ACTYPTAB)     Set to unknown                               
FINDTY80 LR    R1,R5                                                            
         XIT1  REGS=(R1)                                                        
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* Report by record type match vs differences/ print report                      
***********************************************************************         
         USING TYPD,R5                                                          
RECRPT   NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,ISREPORT         Print report                                 
         JNE   RECRPT20                                                         
         BRAS  RE,ZPRINTNG         Blank line                                   
         MVC   P,SPACES                                                         
         MVC   P+1(HD1LNQ),HD1                                                  
         BRAS  RE,ZPRINTNG                                                      
                                                                                
         USING RPTD,R2                                                          
         L     R5,=A(ACTYPTAB)                                                  
         LA    R2,P                                                             
RECRPT10 CLI   0(R5),X'FF'         End of table                                 
         JE    RECRPT18            Done                                         
         MVC   RPTNAME,TYPNAME                                                  
         MVI   RPTEQL,C'='                                                      
         ICM   R1,15,TYPTOLD                                                    
         CVD   R1,DUB                                                           
         MVC   RPTTOLD,=X'402020202020212020'                                   
         OI    DUB+L'DUB-1,X'0F'                                                
         ED    RPTTOLD,DUB+4                                                    
                                                                                
         ICM   R1,15,TYPTNEW                                                    
         CVD   R1,DUB                                                           
         MVC   RPTTNEW,=X'402020202020212020'                                   
         OI    DUB+L'DUB-1,X'0F'                                                
         ED    RPTTNEW,DUB+4                                                    
                                                                                
         ICM   R1,15,TYPKMTCH                                                   
         CVD   R1,DUB                                                           
         MVC   RPTKMTCH,=X'402020202020212020'                                  
         OI    DUB+L'DUB-1,X'0F'                                                
         ED    RPTKMTCH,DUB+4                                                   
                                                                                
         ICM   R1,15,TYPKDIFF                                                   
         CVD   R1,DUB                                                           
         MVC   RPTKDIFF,=X'402020202020212020'                                  
         OI    DUB+L'DUB-1,X'0F'                                                
         ED    RPTKDIFF,DUB+4                                                   
                                                                                
         ICM   R1,15,TYPEMTCH                                                   
         CVD   R1,DUB                                                           
         MVC   RPTEMTCH,=X'402020202020212020'                                  
         OI    DUB+L'DUB-1,X'0F'                                                
         ED    RPTEMTCH,DUB+4                                                   
                                                                                
         ICM   R1,15,TYPEDIFF                                                   
         CVD   R1,DUB                                                           
         MVC   RPTEDIFF,=X'402020202020212020'                                  
         OI    DUB+L'DUB-1,X'0F'                                                
         ED    RPTEDIFF,DUB+4                                                   
                                                                                
         GOTOR ZPRINTNG            Print blank line                             
         AHI   R5,TYPLNQ                                                        
         J     RECRPT10            Next entry                                   
                                                                                
RECRPT18 GOTOR ZPRINTNG            Print blank line                             
         J     RECRPTX                                                          
         DROP  R2                                                               
                                                                                
*--------------------------------------*                                        
*  Keys matched, element didn't        *                                        
*--------------------------------------*                                        
RECRPT20 CHI   R1,ISDIFELM         Keys matched, now,element checking           
         JNE   RECRPT30            Branch if keys didn't match                  
         LLC   R5,RECTYPE                                                       
         GOTOR FINDTYP,(R5)                                                     
         LR    R5,R1               Point to table entry                         
         ICM   R3,15,TYPKMTCH      Key matched                                  
         AHI   R3,1                                                             
         STCM  R3,15,TYPKMTCH                                                   
         ICM   R3,15,TYPEDIFF      Not element                                  
         AHI   R3,1                                                             
         STCM  R3,15,TYPEDIFF                                                   
         J     RECRPTX                                                          
                                                                                
*------------------------------*                                                
*  Have old record not new     *                                                
*------------------------------*                                                
RECRPT30 CHI   R1,ISOLD            Count # of old                               
         JNE   RECRPT40                                                         
         LLC   R5,RECTYOLD                                                      
         GOTOR FINDTYP,(R5)                                                     
         LR    R5,R1               Point to table entry                         
         ICM   R3,15,TYPTOLD       Update old record count                      
         AHI   R3,1                                                             
         STCM  R3,15,TYPTOLD                                                    
         J     RECRPTX                                                          
                                                                                
*------------------------------*                                                
*  Have new record not old     *                                                
*------------------------------*                                                
RECRPT40 CHI   R1,ISNEW            Count # of old                               
         JNE   RECRPT50                                                         
         LLC   R5,RECTYNEW                                                      
         GOTOR FINDTYP,(R5)                                                     
         LR    R5,R1               Point to table entry                         
         ICM   R3,15,TYPTNEW       Update new record count                      
         AHI   R3,1                                                             
         STCM  R3,15,TYPTNEW                                                    
         J     RECRPTX                                                          
                                                                                
*------------------------------*                                                
*  Differnt keys               *                                                
*------------------------------*                                                
RECRPT50 CHI   R1,ISDIFKEY         Keys are different                           
         JNE   RECRPT60                                                         
         AP    DIFFERS,=P'1'                                                    
         LLC   R5,RECTYNEW                                                      
         GOTOR FINDTYP,(R5)                                                     
         LR    R5,R1               Point to table entry                         
         ICM   R3,15,TYPKDIFF      Update new record count                      
         AHI   R3,1                                                             
         STCM  R3,15,TYPKDIFF                                                   
         J     RECRPTX                                                          
                                                                                
*----------------------------*                                                  
*Element now matched too     *                                                  
*----------------------------*                                                  
RECRPT60 CHI   R1,ISOKAY           If equal then okay so far                    
         JE    *+6                                                              
         DC    H'00'                                                            
         AP    MATCHED,=P'1'                                                    
                                                                                
         LLC   R5,RECTYPE                                                       
         GOTOR FINDTYP,(R5)                                                     
         LR    R5,R1               Point to table entry                         
         ICM   R3,15,TYPKMTCH      Matched key                                  
         AHI   R3,1                                                             
         STCM  R3,15,TYPKMTCH                                                   
         ICM   R3,15,TYPEMTCH      Matched element                              
         AHI   R3,1                                                             
         STCM  R3,15,TYPEMTCH                                                   
                                                                                
RECRPTX  J     XIT                                                              
         DROP  R5,R8                                                            
                                                                                
*---------------------------*                                                   
*    Headings for report    *                                                   
*---------------------------*                                                   
HD1      DC    C'Type     '                                                     
         DC    C'Old recs  '                                                    
         DC    C'New recs  '                                                    
         DC    C'Keys okay '                                                    
         DC    C'Keys diff '                                                    
         DC    C'Elem okay '                                                    
         DC    C'Elem diff '                                                    
HD1LNQ   EQU   *-HD1                                                            
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                                                               
                                                                                
         LTORG                                                                  
                                                                                
COMFACS  DS    0F                                                               
DATAMGR  DC    V(DATAMGR)          Only item in comfacs set correct             
                                                                                
CARDS    DC    V(CARDS)                                                         
CPRINT   DC    V(CPRINT)                                                        
DECODE   DC    V(DECODE)                                                        
ACRECTYP DC    V(ACRECTYP)                                                      
DATVAL   DC    V(DATVAL)                                                        
DATCON   DC    V(DATCON)                                                        
HEXIN    DC    V(HEXIN)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
PDUMPER  DC    V(PDUMPER)                                                       
PRINT    DC    V(PRINT)                                                         
PRINTER  DC    V(PRINTER)                                                       
SCANNER  DC    V(SCANNER)                                                       
BUFFRIN  DC    V(BUFFERIN)                                                      
*ORTER   DC    V(SORTER)                                                        
STXITER  DC    V(STXITER)                                                       
*                                                                               
ADMGR    DC    A(DMGR)                                                          
DMRD     EQU   1                   READ                                         
DMHI     EQU   2                   HIGH                                         
DMSEQ    EQU   3                   SEQUENTIAL                                   
DMGET    EQU   4                   GET RECORD                                   
*                                                                               
AIO1     DC    A(IO1)              IO AREA 1                                    
ATAPEOLD DC    A(TAPEOLD)                                                       
ATAPENEW DC    A(TAPENEW)                                                       
ARCVOLD  DC    A(RCVOLD)                                                        
ARCVNEW  DC    A(RCVNEW)                                                        
AOLDFILE DC    A(OLDREC)                                                        
ANEWFILE DC    A(NEWREC)                                                        
AFILETAB DC    A(FILTAB)                                                        
*                                                                               
SORTSW   DC    X'00'                                                            
SORTINT  EQU   X'80'               . Sort is on or initialized                  
*                                                                               
ANYDIFF  DS    C                   Yes/No                                       
ONLYONE  DS    C                   Yes/No                                       
HEXPLNQ  EQU   40                                                               
*                                                                               
ALPHA    DC    CL2'  '                                                          
*                                                                               
SVRE     DS    A                                                                
SVZRE    DS    A                   This is used for ZPRINTNG                    
PARM     DC    6F'0'                                                            
DMCB     DC    6F'0'                                                            
FULL     DC    F'0'                                                             
DUB      DC    D'0'                                                             
HALF     DC    H'0'                                                             
BYTE     DC    X'00'                                                            
WORK     DC    CL100' '                                                         
*                                                                               
CARDH    DS    XL8                 HEADER FOR CARD                              
         ORG   CARDH                                                            
         DC    AL1(L'CARDH+L'CARDIO)                                            
         DC    XL4'00'                                                          
         DC    AL1(L'CARDIO)                                                    
         DC    XL2'00'                                                          
CARDIO   DC    CL80' '                                                          
*                                                                               
#OFKEYS  DC    X'00'               NUMBER OF KEY STARTS                         
#OFKEYE  DC    X'00'               NUMBER OF KEY ENDS                           
KEYSTRT  DC    (MAXKEYS)XL42'00'                                                
KEYEND   DC    (MAXKEYS)XL42'FF'                                                
*                                                                               
UPSI     DS    X                                                                
UPSIDMPR EQU   X'80'                                                            
*                                                                               
DKEY     DS    CL42                                                             
DIR      DS    CL64                                                             
DA       DS    F                                                                
*                                                                               
DMWORK   DC    24F'0'                                                           
DMBYTE   DC    X'00'                                                            
SPACE    DC    CL132' '                                                         
*                                                                               
TODAYMD  DS    CL6                 Today Character YYMMDD                       
TODAYP   DS    XL3                 Today packed                                 
TODAYC   DS    XL2                 Today compressed                             
TODAYX   DS    XL2                 Today compressed complement                  
TODAYB   DS    XL3                 Today binary                                 
                                                                                
DATEYMD  DS    CL6                 Date is (DATE=)                              
DATEP    DS    XL3                 Date packed                                  
DATEC    DS    XL2                 Date compressed                              
DATEX    DS    XL2                 Date compressed complement                   
DATEB    DS    XL3                 Date binary                                  
                                                                                
DATEDSP  DC    AL1(TODAYMD-TODAYMD)                                             
         DC    AL1(TODAYP-TODAYMD)                                              
         DC    AL1(TODAYC-TODAYMD)                                              
         DC    AL1(TODAYX-TODAYMD)                                              
         DC    AL1(TODAYB-TODAYMD)                                              
                                                                                
         ENTRY UTL                                                              
UTL      DC    F'0',X'0A'                                                       
         ORG   UTL+4                                                            
SE       DS    X                                                                
*                                                                               
#OFFILES DC    AL1(2)              Hard coded for now                           
FILELIST DC    10X'00'                                                          
         ORG   FILELIST                                                         
         DC    X'696A'             Hard coded for now                           
         ORG                                                                    
*                                                                               
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMGETR   DC    C'GETREC '                                                       
OPEN     DC    C'OPEN   '                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
CTFILEL  DC    C'NCTFILE X'                                                     
ACCOUNT  DC    C'ACCOUNT'                                                       
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
*                                                                               
OLDFILE  DC    AL1(TAPE+INPOLD)                                                 
NEWFILE  DC    AL1(TAPE+INPNEW)                                                 
DISK     EQU   X'80'                                                            
TAPE     EQU   X'40'                                                            
RECOVERY EQU   X'20'                                                            
EOF      EQU   X'04'                                                            
INPOLD   EQU   X'01'                                                            
INPNEW   EQU   X'02'                                                            
                                                                                
INPUT    DC    AL1(0)                                                           
START    EQU   X'08'                                                            
END      EQU   X'04'                                                            
NEXT     EQU   X'01'                                                            
*                                                                               
SHOWDET  DC    AL1(NO)             Yes/No - show detailed difference            
*                                                                               
DMPL1    DC    AL4(ACCOMP),X'80',VL3(DUMMY)                                     
#OFOLD   DC    PL8'0'                                                           
#OFNEW   DC    PL8'0'                                                           
MAXDMP   DC    F'500'                                                           
BLKSZ    DC    H'32760'                                                         
KEYLEN   DC    H'0'                                                             
KEYDEL   DC    H'0'                                                             
KEYCMPLN DC    H'0'                                                             
DIFFSTAT DC    AL1(NO)                                                          
SVTYP    DS    AL1                                                              
RECTYPE  DS    AL1                                                              
RECTYOLD DS    AL1                                                              
RECTYNEW DS    AL1                                                              
COMPANY  DS    X                                                                
IGDEL    DC    AL1(NO)             Ignore deletes                               
RECLN    DC    H'4004'                                                          
REC#OLD  DC    PL8'0'                                                           
REC#NEW  DC    PL8'0'                                                           
TOT#OLD  DC    PL8'0'                                                           
TOT#NEW  DC    PL8'0'                                                           
STATCNT  DC    PL8'0'                                                           
MATCHED  DC    PL8'0'                                                           
DIFFERS  DC    PL8'0'                                                           
TOTAL    DC    PL8'0'                                                           
PRINTED1 DS    A                   Amount printed this pass, old elem           
PRINTED2 DS    A                   Amount printed this pass, new elem           
                                                                                
BUFFACT  DS    AL1                 Buffrin action                               
*                                                                               
DOIT     DS    X                   Keep doing  while these bits are on          
DO1      EQU   X'01'               .  Printing old element                      
DO2      EQU   X'02'               .  Printing new element                      
*                                                                               
IS_DATA  DS    X                                                                
IS_IS    EQU   X'80'               Is IS record  (Index Sequencial)             
IS_DA    EQU   X'40'               Is DA record  (Direct Access)                
IS_PASS  EQU   X'20'               Is Passive key                               
IS_DIRO  EQU   X'10'               Is Specail type 1 directory only             
IS_ARCV  EQU   X'08'               Is Specail type 2 Archive                    
*                                                                               
ISOLD    EQU   1                   Old file record                              
ISNEW    EQU   2                   New file record                              
ISDIFKEY EQU   3                                                                
ISDIFELM EQU   4                                                                
ISOKAY   EQU   5                   Matched                                      
ISREPORT EQU   6                   Report out record types                      
*                                                                               
CC       DS    AL1                 Used to set condition code                   
PCAP     DS    CL6                                                              
P2D      DC    C'2D'                                                            
*                                                                               
RCVCPY   EQU   1                   Recovery copy                                
RCVCHG   EQU   2                   Recovery change                              
RCVADD   EQU   3                   Recovery add                                 
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOR      EQU   0                                                                
TURNOFF  EQU   X'FF'                                                            
ALL      EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
EFFS     DC    X'FFFFFFFFFFFFFF'                                                
MAXKEYS  EQU   6                                                                
*                                                                               
SORTCARD DC    C'SORT FIELDS=(5,100,A),FORMAT=BI,WORK=1  '                      
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
                                                                                
***********************************************************************         
* DCB AND IO AREA                                                     *         
***********************************************************************         
KEYBUFF  BUFFD TYPE=D,KEYLEN=L'ACCKEY+4,COMLEN=4*K,BUFFERS=1000,       +        
               FILE=TEMPWRK                                                     
                                                                                
*APEOLD  DCB   DDNAME=TAPEOLD,DSORG=PS,EODAD=GETIEOF,RECFM=VB,         +        
               LRECL=6024,BUFNO=2,BLKSIZE=32760,MACRF=(GM,PM)                   
                                                                                
*APENEW  DCB   DDNAME=TAPENEW,DSORG=PS,EODAD=GETIEOF,RECFM=VB,         +        
               LRECL=6024,BUFNO=2,BLKSIZE=32760,MACRF=(GM,PM)                   
                                                                                
TAPEOLD  DCB   DDNAME=TAPEOLD,DSORG=PS,EODAD=GETIEOF,RECFM=VB,         +        
               BUFNO=2,MACRF=(GM)                                               
                                                                                
TAPENEW  DCB   DDNAME=TAPENEW,DSORG=PS,EODAD=GETIEOF,RECFM=VB,         +        
               BUFNO=2,MACRF=(GM)                                               
                                                                                
RCVOLD   DCB   DDNAME=RCVOLD,DSORG=PS,RECFM=VB,BLKSIZE=8204,           +        
               LRECL=8200,MACRF=GM,EODAD=GETIEOF                                
                                                                                
RCVNEW   DCB   DDNAME=RCVNEW,DSORG=PS,RECFM=VB,BLKSIZE=8204,           +        
               LRECL=8200,MACRF=GM,EODAD=GETIEOF                                
         EJECT                                                                  
***********************************************************************         
* Stats table                                                                   
***********************************************************************         
OPTTAB   DS    0CL16               CONTROL CARDS                                
         DC    AL1(3),CL11'OLD       ',AL4(VINPUT1)                             
         DC    AL1(3),CL11'NEW       ',AL4(VINPUT2)                             
         DC    AL1(5),CL11'AGENCY    ',AL4(VAGYC)                               
         DC    AL1(4),CL11'START     ',AL4(VSTRC)                               
         DC    AL1(2),CL11'END       ',AL4(VENDC)                               
         DC    AL1(6),CL11'MAXDUMP   ',AL4(VMXDMP)                              
         DC    AL1(3),CL11'UPSI      ',AL4(VUPSI)                               
         DC    AL1(4),CL11'BLOCK     ',AL4(VBLKSZ)                              
         DC    AL1(2),CL11'LEN       ',AL4(VRECL)                               
         DC    AL1(6),CL11'DETAIL    ',AL4(VDETAIL)                             
         DC    AL1(5),CL11'TODAY     ',AL4(VTODAY)                              
         DC    AL1(4),CL11'DATE      ',AL4(VDATEIS)                             
         DC    AL1(6),CL11'DELETE    ',AL4(VDEL)                                
*        DC    AL1(7),CL11'RECTYPE   ',AL4(VRECTYP)                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* Stats table                                                                   
***********************************************************************         
STATTAB  DC    0F                                                               
         DC    CL20'Total Matched'                                              
         DC    A(MATCHED)                                                       
         DC    CL20'Total Differences'                                          
         DC    A(DIFFERS)                                                       
         DC    CL20'Total Old records'                                          
         DC    A(TOT#OLD)                                                       
         DC    CL20'Total New records'                                          
         DC    A(TOT#NEW)                                                       
         DC    CL20'Number old processed'                                       
         DC    A(REC#OLD)                                                       
         DC    CL20'Number new processed'                                       
         DC    A(REC#NEW)                                                       
         DC    CL20'Status differences'                                         
         DC    A(STATCNT)                                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* Record type table routines                                                    
***********************************************************************         
RECLIST  DC    512XL1'00'                                                       
         ORG   RECLIST+(ACRTTRN*2)                                              
         DC    AL2(RECKTRN-RECTAB)                                              
         ORG   RECLIST+(ACRTBAT*2)  Batch                                       
         DC    AL2(RECKBAT-RECTAB)                                              
         ORG   RECLIST+(ACRTNBT*2)  Transaction Batch                           
         DC    AL2(RECKTBA-RECTAB)                                              
*&&UK*&& ORG   RECLIST+(ACRTTRNA*2)                                             
*&&UK*&& DC    AL2(RECKTRN-RECTAB)                                              
         ORG                                                                    
                                                                                
*ECTYPES DC    XL256'00'                                                        
*        ORG   RECTYPES+ACRTCHDH                                                
*        DC    AL1(CHDKCCPY-CHDRECD)                                            
*        ORG   RECTYPES+ACRTCAC                                                 
*        DC    AL1(CACKCCPY-CACRECD)                                            
*        ORG   RECTYPES+ACRTTRN                                                 
*        DC    AL1(ROUT1)                                                       
*        ORG   RECTYPES+ACRTTRNA                                                
*        DC    AL1(ROUT1)                                                       
*        ORG   RECTYPES+ACRTBUD                                                 
*        DC    AL1(BUDKCCPY-BUDRECD)                                            
*        ORG   RECTYPES+ACRTTDT                                                 
*        DC    AL1(TSIKCCPY-TSIRECD)                                            
*        ORG   RECTYPES+ACRTTIM                                                 
*        DC    AL1(TIMKCCPY-TIMRECD)                                            
*        ORG                                                                    
         EJECT ,                                                                
***********************************************************************         
* Record table                                                                  
***********************************************************************         
RECTAB   DC    C'RECTAB'                                                        
RECKBAT  DC    AL1(0,L'BATKDATE,RCCLRD2T,RCCLRPDT),AL2(BATKDATE-BATKEY)         
         DC    AL1(EOT)                                                         
                                                                                
RECKTBA  DC    AL1(0,L'TBAKADDT,RCCLRD2T,RCCLRXDT),AL2(TBAKADDT-TBAKEY)         
         DC    AL1(0,L'TBAKBREF,0,0),AL2(TBAKBREF-TBAKEY)                       
         DC    AL1(0,L'TBAKBCHR,0,0),AL2(TBAKBCHR-TBAKEY)                       
         DC    AL1(EOT)                                                         
                                                                                
RECKTRN  DC    AL1(RECKCLR,2,0,0),AL2(TRNKREF+4-TRNKEY)                         
         DC    AL1(0,L'TRNKDATE,RCCLRD2T,RCCLRPDT),AL2(TRNKDATE-TRNKEY)         
         DC    AL1(EOT)                                                         
                                                                                
RECKCLR  EQU   1                                                                
RECKXTR  EQU   2                                                                
WXXX1    DMDA  RECSIZE=13680,DSKXTNT=16                                         
         EJECT ,                                                                
***********************************************************************         
* Element table                                                                 
***********************************************************************         
ELMLIST  DC    1024XL1'00'                                                      
         ORG   ELMLIST+(RSTELQ*4)                         X'30'                 
         DC    AL2(ELMRST-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(TRNELQ*4)                         X'44'                 
         DC    AL2(ELMTRN-ELMTAB),AL1(0,0)                                      
*        ORG   ELMLIST+(SCIELQ*4)                         X'50'                 
*        DC    AL2(ELMSCI-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(TRSELQ*4)                         X'60'                 
         DC    AL2(ELMTRS-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(GLDELQ*4)                         X'63'                 
         DC    AL2(ELMGLD-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(TIDELQ*4)                         X'D6'                 
         DC    AL2(ELMTID-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(RALELQ*4)                         X'D9'                 
         DC    AL2(ELMRAL-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(BHDELQ*4)                         X'E0'                 
         DC    AL2(ELMBHD-ELMTAB),AL1(0,0)                                      
         ORG   ELMLIST+(RACELQ*4)                         X'F?'                 
         DC    AL2(ELMRAC-ELMTAB),AL1(0,0)                                      
         ORG                                                                    
                                                                                
ELMTAB   DC    C'ELMTAB'                                                        
ELMRST   DC    AL1(0,L'RSTTDATE,0,0),AL2(RSTTDATE-RSTELD,0)                     
         DC    AL1(EOT)                                                         
                                                                                
ELMTRN   DC    AL1(0,L'TRNSUB,0,0),AL2(TRNSUB-TRNELD,0)                         
         DC    AL1(0,L'TRNREF-4,0,0),AL2(TRNREF-TRNELD+4,0)                     
         DC    AL1(0,8,TRNTCALC,0)                                              
         DC    AL2(TRNNARR-TRNELD+29,TRNTYPE-TRNELD)                            
         DC    AL1(0,8,TRNTVOID,ELMRTE1)                                        
         DC    AL2(TRNNARR-TRNELD+8,TRNTYPE-TRNELD)                             
         DC    AL1(EOT)                                                         
                                                                                
ELMTRS   DC    AL1(0,L'TRSDATE,0,0),AL2(TRSDATE-TRSELD,0)                       
         DC    AL1(0,L'TRSREVD,0,0),AL2(TRSREVD-TRSELD,0)                       
         DC    AL1(0,L'TRSUPDT,0,0),AL2(TRSUPDT-TRSELD,0)                       
         DC    AL1(0,L'TRSEFDT,0,0),AL2(TRSEFDT-TRSELD,0)                       
         DC    AL1(0,L'TRSUSER,0,0),AL2(TRSUSER-TRSELD,0)                       
         DC    AL1(EOT)                                                         
                                                                                
ELMGLD   DC    AL1(0,L'GLDDATE,0,0),AL2(GLDDATE-GLDELD,0)                       
         DC    AL1(0,L'GLDPRG#,0,0),AL2(GLDPRG#-GLDELD,0)                       
         DC    AL1(0,L'GLDUPTM,0,0),AL2(GLDUPTM-GLDELD,0)                       
         DC    AL1(0,L'GLDTODAY,0,0),AL2(GLDTODAY-GLDELD,0)                     
         DC    AL1(EOT)                                                         
                                                                                
ELMTID   DC    AL1(0,L'TID,0,0),AL2(TID-TIDELD,0)                               
         DC    AL1(EOT)                                                         
                                                                                
ELMRAL   DC    AL1(0,L'RALTDAT,RALTTTO,0)                                       
         DC    AL2(RALTDAT-RALELD,RALTYPE-RALELD)                               
         DC    AL1(0,L'RALTDAT,RALTTFR,0)                                       
         DC    AL2(RALTDAT-RALELD,RALTYPE-RALELD)                               
         DC    AL1(EOT)                                                         
                                                                                
ELMRAC   DC    AL1(0,L'RACTIME,0,0),AL2(RACTIME-RACELD,0)                       
         DC    AL1(0,L'RACDATE,0,0),AL2(RACDATE-RACELD,0)                       
         DC    AL1(EOT)                                                         
                                                                                
ELMBHD   DC    AL1(0,L'BHDLUID,0,0),AL2(BHDLUID-BHDELD,0)                       
         DC    AL1(0,L'BHDIBNO,0,0),AL2(BHDIBNO-BHDELD,0)                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* Table of types and names                                                      
***********************************************************************         
       ++INCLUDE ACRECTAB                                                       
         EJECT                                                                  
         DC    CL8'*OLDREC*'                                                    
OLDRECL  DC    F'0'                                                             
OLDREC   DS    XL(6*K)                                                          
                                                                                
         DC    CL8'*NEWREC*'                                                    
NEWRECL  DC    F'0'                                                             
NEWREC   DS    XL(6*K)                                                          
                                                                                
         DC    CL8'**AIO1**'                                                    
IO1      DS    XL(8*K)                                                          
                                                                                
         EJECT                                                                  
***********************************************************************         
* Definitions of files                                                          
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DMFILTAB                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
RECTABD  DSECT                                                                  
         DS    AL1                                                              
RCCLRLEN DS    AL1                 Clear record length                          
RCCLRIND DS    X                   Special indicators                           
RCCLRD2T EQU   X'80'               .  Match date and set to today               
RCCLROTH DS    AL1                 Other value                                  
         ORG   RCCLROTH                                                         
RCCLRDTE DS    AL1                 Date type for DATCON                         
RCCLRTDT EQU   0                   .  YYMMDD date                               
RCCLRPDT EQU   1                   .  Packed     date                           
RCCLRCDT EQU   2                   .  Compressed date                           
RCCLRXDT EQU   3                   .  Compressed date                           
RCCLRBDT EQU   4                   .  Bindary    date                           
RCCLRDSP DS    AL2                 Clear record displacement                    
RECLNQ   EQU   *-RECTABD                                                        
                                                                                
ELMTABD   DSECT                                                                 
ELCLRTYP DS    AL1                 Record type only                             
ELCLRLEN DS    AL1                 Clear element length                         
ELMTYPE  DS    X                   Type to match on                             
ELMRTE#  DS    X                                                                
ELMRTE1  EQU   1                                                                
ELCLRDSP DS    AL2                 Clear element displacement                   
ELMTYDSP DS    AL2                 Element type  displacement                   
ELMLNQ   EQU   *-ELMTABD                                                        
                                                                                
STATD    DSECT                                                                  
STATTXT  DS    CL20                                                             
STATAMT  DS    A                                                                
STATLNQ  EQU   *-STATD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR SORTING                                                             
***********************************************************************         
SORTD    DSECT                                                                  
SORTLEN  DS    F                                                                
SORTKEY  DS    CL(ACCRFST-ACCRECD)                                              
SORTKXT  DS    CL20                                                             
SORTSEQ# DS    XL1                                                              
SORTOLD# EQU   X'01'                                                            
SORTNEW# EQU   X'02'                                                            
SORTKLNQ EQU   *-SORTKEY                                                        
SORTDATA DS    0C                                                               
                                                                                
***********************************************************************         
* DSECT FOR SCANNER BLOCK                                                       
***********************************************************************         
SCAND    DSECT                                                                  
SCANLLN  DS    X                   LENGTH OF LEFT SIDE                          
SCANRLN  DS    X                   LENGTH OF RIGHT SIDE                         
SCANLST  DS    X                   LEFT STATUS BYTES                            
SCANNUM  EQU   X'80'               NUMERIC                                      
SCANALP  EQU   X'40'               ALPHA                                        
SCANHEX  EQU   X'20'               HEX                                          
SCANRST  DS    X                   RIGHT STATUS BYTES                           
SCANLBV  DS    XL4                 LEFT BINARY VALUE                            
SCANRBV  DS    XL4                 RIGHT BINARY VALUE                           
SCANLFT  DS    CL10                LEFT DATA                                    
SCANRHT  DS    CL40                RIGHT DATA                                   
SCANLNQ  EQU   *-SCAND                                                          
         EJECT                                                                  
***********************************************************************         
* Report printing DSECT for record totals by type                               
***********************************************************************         
RPTD     DSECT                                                                  
         DS    CL1                                                              
RPTNAME  DS    CL8                                                              
RPTEQL   DS    CL1                 C'='                                         
         DS    CL1                                                              
RPTTOLD  DS    CL9                                                              
         DS    CL1                                                              
RPTTNEW  DS    CL9                                                              
         DS    CL1                                                              
RPTKMTCH DS    CL9                                                              
         DS    CL1                                                              
RPTKDIFF DS    CL9                                                              
         DS    CL1                                                              
RPTEMTCH DS    CL9                                                              
         DS    CL1                                                              
RPTEDIFF DS    CL9                                                              
RPTLNQ   EQU   *-RPTD                                                           
                                                                                
***********************************************************************         
* DSECT to cover ACTYPTAB                                                       
***********************************************************************         
TYPD     DSECT                                                                  
TYPNAME  DS    CL8                                                              
TYPCODE  DS    AL1                                                              
TYPTOLD  DS    AL4                 Total old records                            
TYPTNEW  DS    AL4                 Total new records                            
TYPKMTCH DS    AL4                 Total keys matched                           
TYPKDIFF DS    AL4                 Total keys different                         
TYPEMTCH DS    AL4                 Total elements matched                       
TYPEDIFF DS    AL4                 Total elements different                     
TYPLNQ   EQU   *-TYPD                                                           
         EJECT                                                                  
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENRAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDRCVRHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMRCVRHDR                                                      
         PRINT ON                                                               
* DMRCVREXT                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMRCVREXT                                                      
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017DDNEWT    04/08/18'                                      
         END                                                                    
